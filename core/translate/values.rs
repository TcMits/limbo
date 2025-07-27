use crate::translate::emitter::Resolver;
use crate::translate::expr::{translate_expr_no_constant_opt, NoConstantOptReason};
use crate::translate::plan::{QueryDestination, SelectPlan};
use crate::vdbe::builder::ProgramBuilder;
use crate::vdbe::insn::{IdxInsertFlags, Insn};
use crate::vdbe::BranchOffset;
use crate::Result;

pub fn emit_values(
    program: &mut ProgramBuilder,
    plan: &SelectPlan,
    resolver: &Resolver,
) -> Result<usize> {
    if plan.values.len() == 1 {
        let start_reg = emit_values_when_single_row(program, plan, resolver)?;
        return Ok(start_reg);
    }

    let reg_result_cols_start = match plan.query_destination {
        QueryDestination::ResultRows => emit_toplevel_values(program, plan, resolver)?,
        QueryDestination::CoroutineYield { yield_reg, .. } => {
            emit_values_in_subquery(program, plan, resolver, yield_reg)?
        }
        QueryDestination::EphemeralIndex { .. } => emit_toplevel_values(program, plan, resolver)?,
        QueryDestination::EphemeralTable { .. } => unreachable!(),
    };
    Ok(reg_result_cols_start)
}

fn emit_values_when_single_row(
    program: &mut ProgramBuilder,
    plan: &SelectPlan,
    resolver: &Resolver,
) -> Result<usize> {
    let first_row = &plan.values[0];
    let row_len = first_row.len();
    let start_reg = program.alloc_registers(row_len);
    for (i, v) in first_row.iter().enumerate() {
        translate_expr_no_constant_opt(
            program,
            None,
            v,
            start_reg + i,
            resolver,
            NoConstantOptReason::RegisterReuse,
        )?;
    }
    emit_values_to_destination(program, plan, start_reg, row_len);
    Ok(start_reg)
}

fn emit_toplevel_values(
    program: &mut ProgramBuilder,
    plan: &SelectPlan,
    resolver: &Resolver,
) -> Result<usize> {
    let yield_reg = program.alloc_register();
    let definition_label = program.allocate_label();
    let start_offset_label = program.allocate_label();
    program.emit_insn(Insn::InitCoroutine {
        yield_reg,
        jump_on_definition: definition_label,
        start_offset: start_offset_label,
    });
    program.preassign_label_to_next_insn(start_offset_label);

    let start_reg = emit_values_in_subquery(program, plan, resolver, yield_reg)?;

    program.emit_insn(Insn::EndCoroutine { yield_reg });
    program.preassign_label_to_next_insn(definition_label);

    program.emit_insn(Insn::InitCoroutine {
        yield_reg,
        jump_on_definition: BranchOffset::Offset(0),
        start_offset: start_offset_label,
    });
    let end_label = program.allocate_label();
    let goto_label = program.allocate_label();
    program.preassign_label_to_next_insn(goto_label);
    program.emit_insn(Insn::Yield {
        yield_reg,
        end_offset: end_label,
    });
    let row_len = plan.values[0].len();
    let copy_start_reg = program.alloc_registers(row_len);
    for i in 0..row_len {
        program.emit_insn(Insn::Copy {
            src_reg: start_reg + i,
            dst_reg: copy_start_reg + i,
            extra_amount: 0,
        });
    }

    emit_values_to_destination(program, plan, copy_start_reg, row_len);

    program.emit_insn(Insn::Goto {
        target_pc: goto_label,
    });
    program.preassign_label_to_next_insn(end_label);

    Ok(copy_start_reg)
}

fn emit_values_in_subquery(
    program: &mut ProgramBuilder,
    plan: &SelectPlan,
    resolver: &Resolver,
    yield_reg: usize,
) -> Result<usize> {
    let row_len = plan.values[0].len();
    let start_reg = program.alloc_registers(row_len);
    for value in &plan.values {
        for (i, v) in value.iter().enumerate() {
            translate_expr_no_constant_opt(
                program,
                None,
                v,
                start_reg + i,
                resolver,
                NoConstantOptReason::RegisterReuse,
            )?;
        }
        program.emit_insn(Insn::Yield {
            yield_reg,
            end_offset: BranchOffset::Offset(0),
        });
    }

    Ok(start_reg)
}

fn emit_values_to_destination(
    program: &mut ProgramBuilder,
    plan: &SelectPlan,
    start_reg: usize,
    row_len: usize,
) {
    match &plan.query_destination {
        QueryDestination::ResultRows => {
            program.emit_insn(Insn::ResultRow {
                start_reg,
                count: row_len,
            });
        }
        QueryDestination::CoroutineYield { yield_reg, .. } => {
            program.emit_insn(Insn::Yield {
                yield_reg: *yield_reg,
                end_offset: BranchOffset::Offset(0),
            });
        }
        QueryDestination::EphemeralIndex { .. } => {
            emit_values_to_index(program, plan, start_reg, row_len);
        }
        QueryDestination::EphemeralTable { .. } => unreachable!(),
    }
}

fn emit_values_to_index(
    program: &mut ProgramBuilder,
    plan: &SelectPlan,
    start_reg: usize,
    row_len: usize,
) {
    let (cursor_id, index, is_delete) = match &plan.query_destination {
        QueryDestination::EphemeralIndex {
            cursor_id,
            index,
            is_delete,
        } => (cursor_id, index, is_delete),
        _ => unreachable!(),
    };
    if *is_delete {
        program.emit_insn(Insn::IdxDelete {
            start_reg,
            num_regs: row_len,
            cursor_id: *cursor_id,
            raise_error_if_no_matching_entry: false,
        });
    } else {
        let record_reg = program.alloc_register();
        program.emit_insn(Insn::MakeRecord {
            start_reg,
            count: row_len,
            dest_reg: record_reg,
            index_name: Some(index.name.clone()),
        });
        program.emit_insn(Insn::IdxInsert {
            cursor_id: *cursor_id,
            record_reg,
            unpacked_start: None,
            unpacked_count: None,
            flags: IdxInsertFlags::new().no_op_duplicate(),
        });
    }
}
